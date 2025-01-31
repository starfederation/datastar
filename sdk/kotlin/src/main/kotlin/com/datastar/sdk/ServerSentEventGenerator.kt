package com.datastar.sdk

import io.ktor.http.*
import io.ktor.server.sse.*
import java.net.URLDecoder

import io.ktor.server.request.*
import kotlinx.serialization.json.Json

interface ResponseAdapter {
    suspend fun send(dataStarEvent: DataStarEvent)
}

class KtorResponseAdapter(private val serverSSESession: ServerSSESession) : ResponseAdapter {
    override suspend fun send(dataStarEvent: DataStarEvent) {
        serverSSESession.send(
            event = dataStarEvent.eventType,
            data = dataStarEvent.dataLines.joinToString("\n"),
            id = dataStarEvent.id
        )
    }
}

class ServerSentEventGenerator(private val responseAdapter: ResponseAdapter) {

    suspend fun send(dataStarEvent: DataStarEvent) {
        responseAdapter.send(dataStarEvent)
    }

    suspend fun mergeFragments(fragmentsOptions: MergeFragmentsOptions) {
        send(fragmentsOptions.toDataStarEvent())
    }

    suspend fun removeFragments(fragmentsOptions: RemoveFragmentsOptions) {
        send(fragmentsOptions.toDataStarEvent())
    }

    suspend fun mergeSignals(signalsOptions: MergeSignalsOptions) {
        send(signalsOptions.toDataStarEvent())
    }

    suspend fun removeSignals(signalsOptions: RemoveSignalsOptions) {
        send(signalsOptions.toDataStarEvent())
    }

    suspend fun executeScript(scriptOptions: ExecuteScriptOptions) {
        send(scriptOptions.toDataStarEvent())
    }

}

interface RequestAdapter {
    suspend fun readSignals(): String
}

class KtorRequestAdapter(private val request: ApplicationRequest) : RequestAdapter {
    override suspend fun readSignals(): String {
        return when (request.httpMethod) {
            HttpMethod.Get -> {
                // For GET requests, parse datastar query param as URL encoded JSON
                request.queryParameters["datastar"]?.let { URLDecoder.decode(it, "UTF-8") }
                    ?: throw IllegalArgumentException("Missing datastar query parameter")
            }
            else -> {
                // For other methods, read body as JSON string
                request.call.receiveText()
            }
        }
    }
}

val json = Json {
    this.isLenient = true
    this.ignoreUnknownKeys = true
}
suspend inline fun <reified T> readSignals(requestAdapter: RequestAdapter): T {
    val jsonString = requestAdapter.readSignals()
    
    return try {
        json.decodeFromString<T>(jsonString)
    } catch (e: Exception) {
        throw IllegalArgumentException("Invalid JSON format", e)
    }
}
